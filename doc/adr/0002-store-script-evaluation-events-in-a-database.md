# 2. Persist script evaluation events in a database

Date: 2024-10-11

## Status

Accepted

## Context

Currently, the "dump" job extracts script evaluation events together with the associated cost model parameter values and stores the serialized extracted events in batches in the file system (each `*.event` file contains approximately 5000 serialized events).

While this approach is relatively simple to implement, there are several difficulties and problems associated with it:

- There is no mechanism for referential data consistency, so in the case of a one-to-many relationship between cost model parameter values (one) and script evaluation events (many), the former is stored redundantly in every `*.event` file.

- Querying and aggregating such serialized data across files requires writing a specialized program to deserialize event data and apply some function over it. Executing such a program takes time.

- Integration with Grafana would require custom code to publish aggregated results.

### Potential benefits of persisting script evaluation events in a database

1. **Data Consistency**:

   - Ensures referential integrity between cost model parameter values and script evaluation events.
   - Eliminates redundancy by storing cost model parameter values only once.

2. **Efficient Querying and Aggregation**:

   - Facilitates efficient querying and aggregation of data using SQL or other database query languages.
   - Reduces the need for specialized programs to deserialize and process event data.

3. **Scalability**:

   - Databases are designed to handle large volumes of data efficiently.
   - Easier to manage and scale compared to a file-based system.

4. **Integration with Analytics Tools**:

   - Simplifies integration with tools like Grafana for real-time monitoring and visualization.
   - Enables the use of standard database connectors and APIs.

5. **Data Security and Backup**:

   - Provides built-in mechanisms for data security, backup, and recovery.
   - Ensures data durability and protection against data loss.

6. **Performance**:

   - Improves performance for read and write operations compared to file-based storage.
   - Optimizes storage and retrieval times.

7. **Maintainability**:

   - Simplifies maintenance and updates to the data schema.
   - Easier to manage and update compared to a large number of individual files.

8. **Transaction Support**:

   - Supports atomic transactions, ensuring data integrity during concurrent operations.
   - Provides rollback capabilities in case of errors.

Persisting script evaluation events in a database addresses the limitations of the current file-based approach and offers numerous advantages in terms of efficiency, scalability, and maintainability.

## Decision

We will persist script evaluation events in a PostgreSQL database to address the limitations of the current file-based approach. The database is intended to serve both: the Grafana dashboard and ad-hoc queries.

The proposed database schema could be found in the [database](../database) directory.

Other databases like MySQL or InfluxDB were considered, but PostgreSQL was chosen for the following reasons:

- Is open-source and widely used and extensively documented.
- Is a general-purpose RDBMs: supports complex SQL queries, joins, and transactions.
- Suitable for complex data analysis.
- Is simple to set-up.
- Beats MySQL in terms of extensibility and features.
- With the help of extensions like TimescaleDB or pg_partman, it can be used as a time-series database.

It doesn't seem like at this scale a full power of time-series databases like InfluxDB or TimescaleDB is needed.

## Consequences

1. **Maintenance**:

   - The PostgreSQL database needs to be set up and maintained. There is a green flag
     from @zeme-wana.

   - The costs are not expected to be high as the database will not be under the heavy load. The database health will be monitored using Grafana, which offers seamless integration.

2. **Getting data in and out**:

   - The data needs to be inserted into the database and corresponding functionality
     needs to be added to this repository. Much of the "dump" job functionality will be reused.

   - Existing "dump" job will remain operational as a backup.

   - If needed, the database data could be exported for further analysis in a different way: most of the ETL tools support PostgreSQL, as well as stock tools like `pg_dump`.

## Future considerations

In the beginning not all extracted information is stored in a structured way:
While event attributes are stored in dedicated columns, the serialised scripts and their arguments are stored as CBOR blobs.

Storing scripts as CBOR blobs means we can't yet directly answer many interesting questions, e.g.

- What is the average depth of Data objects?
- What are the most commonly used builtin functions?
- What is the cost breakdown between builtin functions and machine costs?

This ADR is a first step towards answering these questions. Further steps could be:

- Extracting the information from the CBOR blobs and storing it in a structured way.
- Adding more columns to the `script_evaluation_events` table to store the extracted information.
- Adding more tables to store the extracted information in a structured way.

In case we decide to store the scripts in a structured way, we'd need to consider the schema evolution process: we would like to avoid having to update the schema frequently (e.g. every Plutus release).

- At the moment, script evaluation events extracted from the mainnet take up approximately 7500 files, totaling around 290 GB.

The question about data growth could be answered by collecting size stats and charting them over time.
