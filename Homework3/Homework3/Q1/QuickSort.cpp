#include <iostream>
#include <stack>

using namespace std;

void swap(int* a, int* b) {
    // Swap two numbers
    int t = *a;
    *a = *b;
    *b = t;
}

int partition(int arr[], int low, int high) {
    // Partition the array
    int pivot = arr[high];
    int i = low;

    for (int j = low; j < high; j++) {
        if (arr[j] <= pivot) {
            swap(&arr[i], &arr[j]); // Swap if element is smaller
            i++;
        }
    }

    swap(&arr[i], &arr[high]); // Swap pivot to correct position
    return i; // Return pivot index
}

void quickSortIterative(int arr[], int first, int last) {
    // Iterative QuickSort using stack
    stack<pair<int, int>> stack;
    stack.push({first, last});

    while (!stack.empty()) {
        auto [low, high] = stack.top();
        stack.pop();

        int pivot_pos = partition(arr, low, high);

        if (pivot_pos - 1 > low) {
            stack.push({low, pivot_pos - 1}); // Push left part
        }

        if (pivot_pos + 1 < high) {
            stack.push({pivot_pos + 1, high}); // Push right part
        }
    }
}

void printArray(int arr[], int size) {
    // Print array elements
    for (int i = 0; i < size; i++) {
        cout << arr[i] << " ";
    }
    cout << endl;
}

int main() {
    int arr[] = {10, 7, 8, 9, 1, 5};
    int n = sizeof(arr) / sizeof(arr[0]);

    cout << "Original array: ";
    printArray(arr, n);

    quickSortIterative(arr, 0, n - 1);

    cout << "Sorted array: ";
    printArray(arr, n);

    return 0;
}
