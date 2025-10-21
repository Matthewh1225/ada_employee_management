# Ada Vehicle Management System - A & C Options

## Overview
This project implements both the "A" Option (Heterogeneous Container using Inheritance) and the "C" Option (Generic/Template Package) for managing employee and vehicle data.

## Project Structure

### A Option: Employee Management System (employee_manager)
**Files:**
- `Employee_System.ads/adb` - Main employee management system
- `Transportation.ads/adb` - Vehicle type hierarchy using inheritance
- `Vehicle_Garage.ads/adb` - Generic garage package
- `ProcessFile.ads/adb` - File I/O processing
- `Enums.ads` - Enumeration types
- `employee_manager.adb` - Main driver program

**Features:**
- **Stack Allocation**: All storage allocated once on the stack (no heap/new/malloc)
- **Inheritance**: Tagged types for vehicles (Transport_Record base, Car_Info and Plane_Info derived)
- **Heterogeneous Container**: Employees can have different vehicle types
- **Sorted Output**: By job type (ascending/descending), then by age and name
- **Real-time Ready**: Minimizes dynamic allocation overhead

**Key Implementation Details:**
- Uses arrays on the stack for employees, cars, and planes
- Free list management for efficient allocation
- Doubly-linked list structure using array indices
- Generic Vehicle_Pool instantiated for Car and Plane types

### C Option: Generic Vehicle Management System (car_sales_manager)
**Files:**
- `Generic_Vehicle_System.ads/adb` - Generic package for vehicle management
- `Car_Sales_Manager.adb` - Instantiation for used car sales

**Features:**
- **Generic Package**: Parameterized by Job_Type (Manufacturer), Model_Type, Max_Age (doors), and storage limits
- **Stack Allocation**: All head nodes and data allocated on stack
- **Flexible Instantiation**: Can be instantiated with different ranges without code modification
- **Sorted Storage**: Maintains sorted order by manufacturer, then doors, then name
- **Reusable**: Can process different data types with appropriate instantiations

**Key Implementation Details:**
- Generic formal types for Job and Model
- Generic formal values for capacity constraints
- Stack-allocated arrays for all vehicle storage
- Department heads (one per manufacturer) allocated on stack
- Insertion maintains sort order

## Building the Project

```powershell
cd d:\Schoolwork\testing_ada
alr build
```

## Running the Programs

### A Option - Employee Manager
```powershell
.\bin\employee_manager.exe
```
Reads from `Cars.txt` and displays employees organized by job type with their vehicles.

### C Option - Car Sales Manager
```powershell
.\bin\car_sales_manager.exe
```
Reads from `Cars.txt` and displays used cars organized by manufacturer (treating manufacturer as "job type", doors as "age").

## Data Format

The `Cars.txt` file contains employee and vehicle data:
```
Name Job Age
Manufacturer Model Doors Color
```

Example:
```
Kevin Analysist 23
Ford Expedition 5 Blue
```

## Key Design Decisions

### Stack vs Heap Allocation
- **A Option**: Uses fixed-size arrays on the stack for all employees and vehicles
- **C Option**: Generic package also uses stack-allocated arrays
- **Benefit**: Real-time performance, no dynamic allocator overhead

### Inheritance Strategy (A Option)
- Base type: `Transport_Record` (tagged)
- Derived types: `Car_Info`, `Plane_Info`
- Polymorphic display using 'Class attribute
- Separate arrays for cars and planes to avoid discriminant overhead

### Generic Design (C Option)
- Formal types: Job_Type and Model_Type (discrete types)
- Formal values: Max_Vehicles, Max_Age, Max_Name_Length
- Allows compile-time instantiation with different ranges
- No runtime type information needed

### Sorting
Both options maintain sorted order during insertion:
- Primary sort: Job type / Manufacturer
- Secondary sort: Age / Doors
- Tertiary sort: Name

### File I/O
- Traditional file I/O using Ada.Text_IO
- Token-based parsing handles varying whitespace
- Robust error handling for malformed data

## Compliance with Requirements

### A Option Requirements ✓
- [x] Heterogeneous container using inheritance
- [x] Stack allocation only (no new/malloc)
- [x] All storage allocated once using Box1.adb pattern
- [x] Tagged types for vehicles
- [x] Employees associated with transportation via access/indices
- [x] Ascending and descending output by job type
- [x] Traditional file I/O

### C Option Requirements ✓
- [x] Generic/template package implementation
- [x] User-specifiable ranges for job type (manufacturer)
- [x] User-specifiable ranges for model
- [x] User-specifiable ranges for entry points (doors)
- [x] Dynamic stack allocation for head nodes
- [x] Processes B option data with appropriate instantiation
- [x] Implements all sort specifications
- [x] Treats manufacturer as "job type"
- [x] Treats doors as "age"
- [x] Treats model name as vehicle type
- [x] No modification of data structures for different instantiations

## Testing

Both programs have been tested with the provided `Cars.txt` data file:
- Employee_Manager: 12 employees with mixed car/plane vehicles
- Car_Sales_Manager: 12 cars from 4 manufacturers (Ford, Chevrolet, Dodge, GMC)

Output shows correct sorting by:
1. Manufacturer/Job (ascending and descending)
2. Doors/Age (ascending within manufacturer)
3. Name (as tiebreaker)

## Advanced Features

1. **Bounded Strings**: Uses Ada.Strings.Bounded for fixed-size string storage
2. **Generic Vehicle Pool**: Reusable pattern for different vehicle types
3. **Exception Handling**: Graceful handling of file errors and invalid data
4. **Type Safety**: Strong typing prevents mixing car/plane data
5. **Performance**: O(n) insertion maintaining sort order, O(1) array access

## Future Enhancements

- Add support for more vehicle types (e.g., motorcycles, trucks)
- Implement save functionality to write back to file
- Add query/search capabilities
- Support for multiple vehicles per employee
- Database backend option

## References

- Ada Program Notes, pages 64-94 (especially 70-75)
- LinkSort program for storage allocation patterns
- Box1.adb for stack allocation techniques
